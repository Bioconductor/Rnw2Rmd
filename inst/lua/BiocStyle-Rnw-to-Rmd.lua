function RawInline (raw)
    local formula = raw.text:match '\\Rpackage{(.*)}'
    if raw.format == 'latex' and formula then
        return pandoc.RawInline('markdown', '`r Biocpkg(' .. formula .. ')`')
    end

    local formula = raw.text:match '\\Robject{(.*)}'
    if raw.format == 'latex' and formula then
        return pandoc.RawInline('markdown', '`' .. formula .. '`')
    end

    local formula = raw.text:match '\\Rfunction{(.*)}'
    if raw.format == 'latex' and formula then
        return pandoc.RawInline('markdown', '`' .. formula .. '`')
    end
end
