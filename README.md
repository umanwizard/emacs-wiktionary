# Emacs Wiktionary browser

[Screenshot of wiktionary.el in action](./ss.png)

## Getting started

To load the package, add this to your `.emacs` file:

``` emacs-lisp
(require 'wiktionary)
```

## Usage

To look up a word, invoke `wiktionary-search-word`.

To navigate to the link under the cursor, hit `RET`.

To go back and forward in the Wiktionary browser, hit `l` and `r`, respectively.

To search for a new word, hit `s`.

## Configuration

These variables are available via Customize:

* `wiktionary-language-order`: list of strings for languages whose results will be ordered
  first. For example, `'("German" "English" "French" "Spanish")` will cause German results to
  appear first, English second, French third, Spanish fourth, and then all other languages
  in alphabetical order following those (assuming `wiktionary-show-unordered-languages` is set).

* `wiktionary-show-unordered-languages`: Whether to show additional languages
  that don't appear in `wiktionary-language-order`. True by default.

