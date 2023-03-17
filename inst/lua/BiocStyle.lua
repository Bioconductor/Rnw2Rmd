-- Create a table of LaTeX commands (the 'key') and the corresponding
-- function to render markdown text. The functions takes two
-- arguments. The first is the LaTeX command, and the second any
-- arguments to the command.

local function ignore(key, args)
   return ''
end

local function emphasis(key, args)
   -- FIXME: validate args as nil or ''
   return '*' .. key .. '*'
end

local function inline_constant(key, args)
   return '`' .. args .. '`'
end

local function inline_r_expression(key, args)
   return '`r ' .. key .. '(' .. args .. ')`'
end

local function inline_r_code(key, args)
   return '`r ' .. args .. '`'
end

latex_to_markdown = {
   -- ignore, e.g., \newpage
   ['newpage'] = ignore,
   ['noindent'] = ignore,

   -- \R{} -> *R*, etc
   ['R'] = emphasis,
   ['Bioconductor'] = emphasis,

   -- \Rpackage{...} -> `...`
   ['Rpackage'] = inline_constant,
   ['pkg'] = inline_constant,   -- from Sweave.sty
   ['Robject'] = inline_constant,
   ['Rfunction'] = inline_constant,
   ['Rcode'] = inline_constant,
   ['code'] = inline_constant,  -- from Sweave.sty
   ['Rclass'] = inline_constant,
   ['software'] = inline_constant,
   ['file'] = inline_constant,

   -- \CRANpkg{...} -> `r CRANpkg(...)`, etc
   ['CRANpkg'] = inline_r_expression,
   ['Biocpkg'] = inline_r_expression,
   ['Githubpkg'] = inline_r_expression,

   -- other, from 'Sweave.sty'
   ['Sexpr'] = inline_r_code
}

return {
   {
      RawInline = function (raw)
         if raw.format ~= 'latex' then
            return raw
         end

         -- use regular expressions to separate the LaTeX command
         -- ('cmd') and arguments ('args')
         local cmd = raw.text:match '^\\([^{ ]+).*'
         local args = raw.text:match '[^{ ]+{(.*)}'

         -- invoke the function corresponding to any defined 'key'
         if latex_to_markdown[cmd] ~= nil then
            local result = latex_to_markdown[cmd](cmd, args)
            return pandoc.RawInline('markdown', result)
         end

         return raw
      end
   }
}

