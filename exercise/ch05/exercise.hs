-- 1. Write a function, fill, with the following type signature.
--    fill :: Int -> Doc -> Doc
-- It should add spaces to a document until it is the given number of columns wide. If it is already wider than this value, it should add no spaces.
fill :: Int -> Doc -> Doc

-- 2. Our pretty printer does not take nesting into account. Whenever we open parentheses, braces, or brackets, any lines that follow should be indented so that they are aligned with the opening character until a matching closing character is encountered.
-- Add support for nesting, with a controllable amount of indentation.

nest :: Int -> Doc -> Doc

