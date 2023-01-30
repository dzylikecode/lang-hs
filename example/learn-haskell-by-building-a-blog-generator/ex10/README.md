# Custom markup language

- Headings: prefix by a number of `*` characters
- Paragraphs: a group of lines without empty lines in between
- Unordered lists: a group of lines each prefixed with `-`
- Ordered lists: a group of lines each prefixed with `#`
- Code blocks: a group of lines each prefixed with `>`

You might ask "Why do we even need to represent the markup as a type? Why don't we convert it into HTML as soon as we parse it instead?". That's a good question and a valid strategy. The reason we first represent it as a Haskell type is for flexibility and modularity.

As stated previously, our strategy for parsing the markup text is:

- Split the string to a list where each element is a separate line (which we can do with [`lines`](https://hackage.haskell.org/package/base-4.16.4.0/docs/Prelude.html#v:lines)), and
- Go over the list line by line and process it, remembering information from previous lines if necessary

[](parse01.hs ":include :type=code hs")

- `currentParagraph`

  积累当前的段落内容

  是类似压栈的方式组合起来的, 所以需要 reverse

- `(5)`

  段落结束的话, 则组合, 清空`currentParagraph`

- `(6)`

  段落未结束, 则继续积累到`currentParagraph`

We pass the new lines to be grouped in a paragraph **in reverse order** because of performance characteristics - because of the nature of singly-linked lists, prepending an element is fast, and appending is slow. Prepending only requires us to create a new cons (`:`) cell to hold a pointer to the value and a pointer to the list, but appending requires us to traverse the list to its end and rebuild the cons cells - the last one will contain the last value of the list and a pointer to the list to append, the next will contain the value before the last value of the list and a pointer to the list which contains the last element and the appended list, and so on.

---

用 Maybe 考虑

[](parse02.hs ":include :type=code hs")
