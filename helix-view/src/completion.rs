use std::{collections::HashMap, sync::Arc};

use anyhow::{Ok, Result};
use futures_util::future::{BoxFuture, Future};
use helix_core::{chars::char_is_word, Rope, RopeSlice};
use helix_lsp::lsp;

#[derive(Debug, PartialEq, Default, Clone)]
pub struct CompletionItem {
    pub item: lsp::CompletionItem,
    pub language_server_id: Option<usize>,
    pub resolved: bool,
}

pub trait CompletionSource: Send + Sync {
    fn request_completion_options(
        self: &Self,
        doc: Rope,
        pos: usize,
    ) -> BoxFuture<Result<Vec<CompletionItem>>>;
}

pub fn request_completion_options(
    completion_sources: impl IntoIterator<Item = Arc<dyn CompletionSource>>,
    doc: Rope,
    pos: usize,
) -> impl Iterator<Item = impl Future<Output = Result<Vec<CompletionItem>>>> {
    completion_sources.into_iter().map(move |source| {
        let doc = doc.clone();
        async move { source.request_completion_options(doc, pos).await }
    })
}

pub struct WordCompletion;

impl CompletionSource for WordCompletion {
    fn request_completion_options(
        self: &Self,
        doc: Rope,
        pos: usize,
    ) -> BoxFuture<Result<Vec<CompletionItem>>> {
        Box::pin(async move {
            let prefix = find_word_prefix(&doc, pos).to_string();
            let completions = find_word_completions(&doc, &prefix);
            Ok(completions
                .iter()
                .map(|(text, _)| CompletionItem {
                    item: lsp::CompletionItem {
                        label: text.to_string(),
                        detail: Some(text.to_string()),
                        kind: Some(lsp::CompletionItemKind::TEXT),
                        ..Default::default()
                    },
                    language_server_id: None,
                    resolved: true,
                })
                .collect())
        })
    }
}

fn find_word_prefix(doc: &Rope, pos: usize) -> RopeSlice {
    let text = doc.slice(..pos);
    let mut iter = text.chars_at(pos).reversed().enumerate();
    let prefix_start = iter
        .find(|(_, char)| !char_is_word(*char))
        .map(|(i, _)| pos - i)
        .unwrap_or(0);
    text.slice(prefix_start..)
}

fn find_word_completions<'a>(doc: &'a Rope, prefix: &str) -> Vec<(&'a str, u8)> {
    const MAX_SCAN_SIZE: usize = 4 * 1024 * 1024;
    const MAX_COMPLETION_OPTIONS: usize = 256;
    const MAX_WORD_LEN: usize = 256; // in bytes

    let mut remaining_scan_size: usize = MAX_SCAN_SIZE;
    let mut chunks = doc.chunks();
    let mut matches: HashMap<&str, u8> = HashMap::new();
    loop {
        let chunk = if let Some(chunk) = chunks.next() {
            if chunk.len() > remaining_scan_size {
                &chunk[..remaining_scan_size]
            } else {
                chunk
            }
        } else {
            break; // end of doucment reached
        };
        remaining_scan_size -= chunk.len();
        let mut chars = chunk.char_indices().peekable();
        while let Some((i, char)) = chars.next() {
            if !char_is_word(char) {
                continue; // skip word boundaries
            }
            if chunk.len() <= i + prefix.len() {
                break; // end of chunk reached
            }
            if &chunk[i..(i + prefix.len())] == prefix {
                let mut word: Option<&str> = None;
                while let Some((j, char)) = chars.next() {
                    if j - i > MAX_WORD_LEN {
                        break; // discard word when MAX_WORD_LEN is reached
                    }
                    if !char_is_word(char) || chars.peek() == None {
                        word = Some(&chunk[i..j]);
                        break;
                    }
                }
                if let Some(word) = word {
                    if let Some(count) = matches.get_mut(word) {
                        *count = count.saturating_add(1);
                    } else if word != prefix {
                        matches.insert(word, 0);
                        if matches.len() >= MAX_COMPLETION_OPTIONS {
                            break;
                        }
                    }
                }
            } else {
                while let Some((_, char)) = chars.next() {
                    if !char_is_word(char) {
                        break;
                    }
                }
            }
        }
        if matches.len() >= MAX_COMPLETION_OPTIONS {
            break;
        }
    }

    let mut matches: Vec<_> = matches.drain().collect();
    matches.sort_unstable_by(|(_, a), (_, b)| b.cmp(a));
    matches
}

#[cfg(test)]
mod test {
    use helix_core::Rope;

    use super::{find_word_completions, find_word_prefix};

    #[test]
    fn find_word_prefix_empty_rope() {
        let rope = Rope::new();
        let prefix = find_word_prefix(&rope, 0);
        assert_eq!(prefix.len_bytes(), 0);
    }

    #[test]
    fn find_word_prefix_cursor_on_boundary() {
        let rope = Rope::from_str("t ");
        let prefix = find_word_prefix(&rope, 2);
        assert_eq!(prefix.len_bytes(), 0);
    }

    #[test]
    fn find_word_prefix_second_word() {
        let rope = Rope::from_str("abc def");
        let prefix = find_word_prefix(&rope, 7);
        assert_eq!(prefix.as_str(), Some("def"));
    }

    #[test]
    fn find_word_completions_no_match() {
        let rope = Rope::from_str("Lorem ipsum dolor sit amet.");
        let prefix = "xyz";
        let completions = find_word_completions(&rope, prefix);
        assert_eq!(completions.len(), 0);
    }

    #[test]
    fn find_word_completions_multiple() {
        let rope = Rope::from_str("repeat repeat nrepeat report resolve.");
        let prefix = "rep";
        let completions = find_word_completions(&rope, prefix);
        assert_eq!(completions, vec![("repeat", 1), ("report", 0)])
    }

    #[test]
    fn find_word_completions_dont_include_prefix() {
        let rope = Rope::from_str("not\nnothing");
        let prefix = "not";
        let completions = find_word_completions(&rope, prefix);
        assert_eq!(completions, vec![("nothing", 0)])
    }
}
