use proc_macro::{Delimiter, Ident, Span, TokenStream, TokenTree};
use unicode_xid::UnicodeXID;

use std::borrow::Cow;

pub(crate) struct IdentBuilder<'a> {
    data: String,
    trailer: Option<Cow<'a, str>>,
}

impl<'a> IdentBuilder<'a> {
    pub(crate) fn new() -> Self {
        IdentBuilder {
            data: String::new(),
            trailer: None,
        }
    }

    pub(crate) fn add_fragment(
        &mut self,
        fragment: &str,
        introducer: Option<&str>,
        leader: Option<&str>,
        trailer: Option<Cow<'a, str>>,
    ) {
        if fragment.is_empty() {
            return;
        }
        if let Some(prev_trailer) = self.trailer.take() {
            self.data += prev_trailer.as_ref();
        }
        if self.data.is_empty() {
            if let Some(introducer) = introducer {
                self.data += introducer;
            }
        } else {
            self.data.push('_');
            if let Some(leader) = leader {
                self.data += leader;
            }
        }
        self.data += fragment;
        self.trailer = trailer;
    }

    pub(crate) fn finish(self) -> Option<Ident> {
        // in the worst case, nothing meaningful has been provided.
        let meaning_less = self.data.is_empty();
        if meaning_less {
            return None;
        }
        let ident = Ident::new(&self.data, Span::call_site());
        Some(ident)
    }
}

#[allow(clippy::match_single_binding)]
fn generate_char_fragment(ch: char) -> Cow<'static, str> {
    match ch {
        _ => {
            let string = format!("char{:04x}", ch as u32);
            string.into()
        }
    }
}

impl<'a> IdentBuilder<'a> {
    pub(crate) fn add_tokenize_and_escaped(
        &mut self,
        sequence: &str,
        introducer: Option<&str>,
        leader: Option<&str>,
        trailer: Option<Cow<'a, str>>,
    ) {
        let mut peeked = sequence.chars().peekable();
        if peeked.peek().is_none() {
            return;
        }
        let mut is_first = true;
        loop {
            let is_last;

            let first_ch = peeked.next().unwrap();
            let fragment;
            let is_ident;
            if first_ch.is_xid_start() {
                let mut seq = String::new();
                seq.push(first_ch);
                loop {
                    match peeked.peek() {
                        None => break,
                        Some(ch) if ch.is_xid_continue() => break,
                        Some(ch) => {
                            seq.push(*ch);
                            let _ = peeked.next();
                        }
                    }
                }
                fragment = Cow::Owned(seq);
                is_ident = true;
            } else {
                fragment = generate_char_fragment(first_ch);
                is_ident = false;
            }
            is_last = peeked.peek().is_none();

            let cur_introducer = if is_first { introducer } else { None };
            let cur_leader = if is_first {
                leader
            } else if is_ident {
                None
            } else {
                Some("_")
            };
            let cur_trailer = if is_last {
                trailer.clone()
            } else if is_ident {
                None
            } else {
                Some("_".into())
            };

            self.add_fragment(fragment.as_ref(), cur_introducer, cur_leader, cur_trailer);

            is_first = false;
        }
    }
}

fn fragment_from_delimiter(d: Delimiter, opening: bool) -> &'static str {
    match (d, opening) {
        (Delimiter::Parenthesis, true) => "(",
        (Delimiter::Parenthesis, false) => ")",
        (Delimiter::Brace, true) => "{",
        (Delimiter::Brace, false) => "}",
        (Delimiter::Bracket, true) => "[",
        (Delimiter::Bracket, false) => "]",
        (Delimiter::None, _) => "",
    }
}

impl<'a> IdentBuilder<'a> {
    pub(crate) fn append_tokentree(&mut self, tt: &TokenTree) {
        match tt {
            TokenTree::Group(g) => {
                let d = g.delimiter();
                self.add_tokenize_and_escaped(
                    fragment_from_delimiter(d, true),
                    Some("grp_"),
                    Some("_"),
                    Some("_".into()),
                );
                for inner_tt in g.stream() {
                    self.append_tokentree(&inner_tt);
                }
                self.add_tokenize_and_escaped(
                    fragment_from_delimiter(d, false),
                    Some("grp_"),
                    Some("_"),
                    Some("_".into()),
                );
            }
            TokenTree::Ident(ident) => {
                let s = ident.to_string();
                self.add_tokenize_and_escaped(&s, None, None, None);
            }
            TokenTree::Punct(punct) => {
                let s = punct.to_string();
                self.add_tokenize_and_escaped(&s, Some("punct_"), Some("_"), Some("_".into()));
            }
            TokenTree::Literal(literal) => {
                let s = literal.to_string();
                self.add_tokenize_and_escaped(&s, Some("lit_"), None, None);
            }
        }
    }
}

fn ident_from_tokenstream(ts: TokenStream) -> Ident {
    let mut ident_name = IdentBuilder::new();
    for tt in ts {
        ident_name.append_tokentree(&tt);
    }
    ident_name.finish().unwrap()
}

#[proc_macro]
pub fn literate(ts: TokenStream) -> TokenStream {
    let tt: TokenTree = ident_from_tokenstream(ts).into();
    tt.into()
}
