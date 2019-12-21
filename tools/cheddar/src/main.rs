use std::env;
use std::ffi::OsStr;
use std::io::BufRead;
use std::io;
use std::path::Path;
use syntect::easy::HighlightLines;
use syntect::dumps::from_binary;
use syntect::highlighting::ThemeSet;
use syntect::parsing::SyntaxSet;
use lazy_static::lazy_static;

use syntect::html::{
    append_highlighted_html_for_styled_line,
    start_highlighted_html_snippet,
    IncludeBackground,
};

// Set up syntaxes as a lazy_static. Initialisation might not be
// required in the case of Markdown rendering (if there's no code
// blocks within the document).
lazy_static! {
    static ref SYNTAXES: SyntaxSet = from_binary(include_bytes!(env!("BAT_SYNTAXES")));
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

fn format_markdown() {
    unimplemented!("Not able to format Markdown just yet");
}

fn format_code(extension: String) {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();
    let mut linebuf = String::new();

    // Get the first line, we might need it for syntax identification.
    let mut read_result = stdin.read_line(&mut linebuf);

    // Set up the highlighter
    let ts = ThemeSet::load_defaults();
    let theme = &ts.themes["InspiredGitHub"];

    let syntax = SYNTAXES.find_syntax_by_extension(&extension)
        .or_else(|| SYNTAXES.find_syntax_by_first_line(&linebuf))
        .unwrap_or_else(|| SYNTAXES.find_syntax_plain_text());

    // We're doing a completely different thing if the file is
    // Markdown, so lets do that thing.
    if syntax.name == "markdown" {

    }

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
