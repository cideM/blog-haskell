---
title: Editors
date: "2018-09-10"
---

_Almost everything in this post applies to Vim as well, but since I use Neovim
exclusively I'll refer to that, instead of Vim_

## Introduction

Neovim is more than just a text editor. It's the combination of modal editing
paradigm, vimscript as a configuration and scripting language, and the actual
program. Unlike in many other editors and IDEs, all three aspects are
intertwined, meaning that a couple of lines of configuration can, over time,
morph into a plugin, with options, functions and mappings. That just doesn't
happen in Visual Studio Code, where configuration is just an object, or Intellj
editors, where you toggle options exposed to you in a GUI.

In my opinion, that kind of customizability is at the heart of what makes
Neovim so special (I assume the same applies to Emacs). Comparing Neovim to
other editors based on a checklist of features is therefore missing the point.
Neovim's syntax highlighting is lacking. Its built-in completion is a blocking
operation. The included file explorer comes with tons of gotchas. It doesn't
have a plugin manager. Very few commands give you any visual feedback until you
execute them. So why should anyone put up with all of that?

If you check any Neovim related post on hackernews, you'll inevitably come
across two arguments: it uses very few resources and it's ubiquitous (or rather
Vim is). Both arguments are correct, but I don't think that they're particulary
strong. Sublime Text is an extremely lean and efficient editor, and I don't
think that Vims universal availability is a result of its sheer excellence.

So again, what's the appeal?

## Your Editor

Neovim's core strength is its customizability. If there's a task that you find
yourself repeating over and over again, turn it into a mapping. If a mapping
doesn't do it, create a function. Your first instinct should not be to go and
find a plugin that does everything for you. In many editors there's a gap
between what you can achieve through configuration and what's possible through
plugins, which require a rather big upfront investment. By relying on plugins
in Neovim as well, you're recreating that boundary, you're still operating
under the same constraint as you would in other editors.

I'm not advocating against the use of plugins by the way. Rather I encourage
people to start their Neovim journey by reading `:help`, by buying a book
(_Learn Vimscript the Hard Way_) and by starting with a [minimal .vimrc
(init.vim in Neovim)](https://github.com/romainl/idiomatic-vimrc). Using Neovim
means using Vimscript. Learning Neovim therefore means learning Vimscript. If
you come across a plugin that does pretty much exactly what you want in the way
you want it to, by all means, use it! The final goal should be an editor
tailored to your needs. Based on your mappings, your functions, your plugins,
enhanced with plugins other people wrote.

There are at least two advantages to this approach: plugins often handle
various use cases, editor versions and environments. That necessitates more
lines of code than if you wrote the same thing, but for your particular use
case, editor version and environment. Less code is usually easier to understand
than more code. Additionally, you get to customize the editor so it solves
exactly your problem. With plugins, you often need to adapt your workflow, so
it fits the way the plugin solves your problem.

## Examples

### Linting

Linting is tricky business. Some languages like Rust and Haskell output
(beautiful) multiline error messages. Some editors struggle with fitting those
errors into their error list or inline popup windows. Neovim's quickfix list
isn't exactly stellar at that either. I therefore have a combination of
mappings and configuration options so that I always get the most of the
compilers and linters I use.

For something simple like ESlint, which mostly outputs oneliners, I use the
built-in `makeprg` option for example:

```
let b:undo_ftplugin="setlocal makeprg< formatprg<"

let b:eslint_exe = trim(system("npm bin")) . "/eslint"

if executable(b:eslint_exe)
    let &l:makeprg=b:eslint_exe . ' --format unix --fix %'
end

if executable("prettier")
    let &l:formatprg = "prettier --config-precedence file-override"
end
```

Here I'm just telling Neovim to run my local eslint whenever I run `:make`.
Thanks to the `--format unix`, Neovim automatically translates the errors into
quickfix list entries.

As an alternative, I also have this mapping:

```
nnoremap <silent> <leader>l :exe 'split<bar>terminal set ESLINT_EXEC_PATH (npm bin); "$ESLINT_EXEC_PATH"/eslint ' . expand('%')<cr><c-w><c-p>
```

It opens a split with a `:terminal` and runs ESlint on the current file.

In Haskell I just open a tmux split for `ghcid` and I do the same in Rust with
`cargo check`.

It's simple, leverages existing functionality if possible, but flexible enough
to adjust to different compilers and linters.

### Testing

Even less fancy. When I'm in a test file I can hit `<leader>t` to automatically
run `jest` on the current file. Most of the time I'm using `jest --watch` or I
have `entr` to watch certain files and just run `stack test` whenever they
change.

### File Navigation

I admit that the vanilla way is pretty slow but the thing is that it's totally
fine for me. I hit `<leader>e` and I get a command prompt with `:e
[CURSOR]/**/*`. My cursor position is indicated by [CURSOR]. I enter the base
folder where I want to search (easier than adding dozens of folders per
language) and then I just rely on tab completion. I am yet to work on repo so
big that this is not instantaneous.
