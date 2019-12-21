use comrak::nodes::{AstNode, NodeValue, NodeHtmlBlock};
use comrak::{Arena, parse_document, format_html, ComrakOptions};
use lazy_static::lazy_static;
use std::env;
use std::ffi::OsStr;
use std::io::BufRead;
use std::io::Read;
use std::io;
use std::path::Path;
use syntect::dumps::from_binary;
use syntect::easy::HighlightLines;
use syntect::highlighting::ThemeSet;
use syntect::parsing::{SyntaxSet, SyntaxReference};

use syntect::html::{
    IncludeBackground,
    append_highlighted_html_for_styled_line,
    highlighted_html_for_string,
    start_highlighted_html_snippet,
};

lazy_static! {
    // Load syntaxes & themes lazily. Initialisation might not be
    // required in the case of Markdown rendering (if there's no code
    // blocks within the document).
    static ref SYNTAXES: SyntaxSet = from_binary(include_bytes!(env!("BAT_SYNTAXES")));
    static ref THEMES: ThemeSet = ThemeSet::load_defaults();

    // Configure Comrak's Markdown rendering with all the bells &
    // whistles!
    static ref MD_OPTS: ComrakOptions = ComrakOptions{
        ext_strikethrough: true,
        ext_tagfilter: true,
        ext_table: true,
        ext_autolink: true,
        ext_tasklist: true,
        ext_header_ids: Some(String::new()), // yyeeesss!
        ext_footnotes: true,
        ext_description_lists: true,
        unsafe_: true, // required for tagfilter
        ..ComrakOptions::default()
    };
}

fn args_extension() -> Option<String> {
    // The name of the file to be formatted is usually passed in as
    // the first argument and can be used to determine a syntax set.
    let args = env::args().collect::<Vec<String>>();
    if args.len() != 2 {
        return None
    }

    Path::new(&args[1]).extension()
        .and_then(OsStr::to_str)
        .map(|s| s.to_string())
}

fn should_continue(res: &io::Result<usize>) -> bool {
    match *res {
        Ok(n) => n > 0,
        Err(_) => false,
    }
}

// This function is taken from the Comrak documentation.
fn iter_nodes<'a, F>(node: &'a AstNode<'a>, f: &F) where F : Fn(&'a AstNode<'a>) {
    f(node);
    for c in node.children() {
        iter_nodes(c, f);
    }
}

// Many of the syntaxes in the syntax list have random capitalisations, which
// means that name matching for the block info of a code block in HTML fails.
//
// Instead, try finding a syntax match by comparing case insensitively (for
// ASCII characters, anyways).
fn find_syntax_case_insensitive(info: &str) -> Option<&'static SyntaxReference> {
    SYNTAXES.syntaxes().iter().rev().find(|&s| info.eq_ignore_ascii_case(&s.name))
}

fn format_markdown() {
    let document = {
        let mut buffer = String::new();
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        stdin.read_to_string(&mut buffer).expect("failed to read stdin");
        buffer
    };

    let arena = Arena::new();
    let root = parse_document(&arena, &document, &MD_OPTS);

    // Syntax highlighting is implemented by traversing the arena and
    // replacing all code blocks with HTML blocks rendered by syntect.
    iter_nodes(root, &|node| {
        let mut ast = node.data.borrow_mut();
        match &ast.value {
            NodeValue::CodeBlock(code_block) => {
                let theme = &THEMES.themes["InspiredGitHub"];
                let info = String::from_utf8_lossy(&code_block.info);

                let syntax = find_syntax_case_insensitive(&info)
                    .or_else(|| SYNTAXES.find_syntax_by_extension(&info))
                    .unwrap_or_else(|| SYNTAXES.find_syntax_plain_text());

                let code = String::from_utf8_lossy(&code_block.literal);
                let rendered = highlighted_html_for_string(
                    &code, &SYNTAXES, syntax, theme,
                );

                let block = NodeHtmlBlock {
                    block_type: 1, // It's unclear what behaviour is toggled by this
                    literal: rendered.into_bytes(),
                };

                ast.value = NodeValue::HtmlBlock(block);
            },
            _ => (),
        };
    });

    format_html(root, &MD_OPTS, &mut io::stdout())
        .expect("Markdown rendering failed");
}

fn format_code(extension: String) {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut linebuf = String::new();

    // Get the first line, we might need it for syntax identification.
    let mut read_result = stdin.read_line(&mut linebuf);

    // Set up the highlighter
    let theme = &THEMES.themes["InspiredGitHub"];

    let syntax = SYNTAXES.find_syntax_by_extension(&extension)
        .or_else(|| SYNTAXES.find_syntax_by_first_line(&linebuf))
        .unwrap_or_else(|| SYNTAXES.find_syntax_plain_text());

    let mut hl = HighlightLines::new(syntax, theme);
    let (mut outbuf, bg) = start_highlighted_html_snippet(theme);

    // Rather than using the `lines` iterator, read each line manually
    // and maintain buffer state.
    //
    // This is done because the syntax highlighter requires trailing
    // newlines to be efficient, and those are stripped in the lines
    // iterator.
    while should_continue(&read_result) {
        let regions = hl.highlight(&linebuf, &SYNTAXES);

        append_highlighted_html_for_styled_line(
            &regions[..],
            IncludeBackground::IfDifferent(bg),
            &mut outbuf,
        );

        // immediately output the current state to avoid keeping
        // things in memory
        print!("{}", outbuf);

        // merry go round again
        linebuf.clear();
        outbuf.clear();
        read_result = stdin.read_line(&mut linebuf);
    }

    println!("</pre>");
}

fn main() {
    let extension = args_extension()
        .expect("cheddar should be invoked with a filename!");

    if extension == "md" {
        format_markdown();
    } else {
        format_code(extension);
    }
}
