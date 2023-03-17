local function convert_start_end_tags(content)
   -- opening '<<...'
   if content[1].text == '<<>>=' then
      content[1].text = '```{r}'
   elseif content[1].text:match('<<.*') then
      content[1].text = content[1].text:gsub('<<', '```{r ')
      -- look for the first Str code chunk ending with '>>='
      for i = 2, #content do
         if content[i].text ~= nil and content[i].text:match '.*>>=' then
            content[i].text = content[i].text:gsub('>>=', '}')
            break
         end
      end
   end

   -- closing '@'
   content[#content].text = '```'

   return content
end

return {
   {
      Para = function(para)
         -- FIXME: assumes paragraph break before and after code chunk
         if not (para.content[1].text ~= nil and
                 para.content[1].text:match '^<<' and
                 para.content[#para.content].text ~= nil and
                 para.content[#para.content].text:match '^@.*')
         then
            return para
         end

         -- replace <<>>= ... @ with fence, 'soft' breaks with line breaks
         para.content = convert_start_end_tags(para.content)
         para = pandoc.walk_block(para, { SoftBreak = pandoc.LineBreak })

         -- convert 'Para' to 'Code'. Is this the best one can do?
         local text = pandoc.write(pandoc.Pandoc(para), 'plain')
         local block = pandoc.RawBlock('markdown', text)

         return block
      end
   }
}
