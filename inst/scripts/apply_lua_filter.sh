pandoc -f latex+raw_tex -t markdown UniProt.ws.Rnw --lua-filter BiocStyle-Rnw-to-Rmd.lua -o UniProt.ws_test.Rmd
