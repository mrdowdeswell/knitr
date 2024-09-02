library(testit)

# see http://stackoverflow.com/q/18992260/559676 for the bug
assert('inline_exec only accept character result', {
  block = list(code = "function() 1", input = "inline `r function() 1`")
  res = xfun::try_silent(inline_exec(block, new.env()))
  (inherits(res, 'try-error'))
  block = list(code = "(function() 1)()", input = "inline `r (function() 1)()`")
  block$location = matrix(c(8,27), ncol = 2, byrow = TRUE)
  res = inline_exec(block, new.env())
  (res %==% "inline 1")
  block = list(code = character(0), input = "no inline")
  res = inline_exec(block, new.env())
  (res %==% "no inline")
})

assert('label_code correct adds comment on code for yaml block or parsed param', {
  oldW = getOption('width')
  options(width = 20)
  (label_code("1+1", list(params.src = "test, eval=TRUE")) %==% "## ----test, eval=TRUE----\n1+1\n")
  options(width = oldW)
  (label_code("1+1", list(params.chunk = c("#| label: test", "#| eval: true"))) %==%
      "## --------\n#| label: test\n#| eval: true\n1+1\n")
})

# see https://github.com/yihui/knitr/issues/1753 for the issue
assert('process_tangle.block parses read_chunk correctly in most cases', {
  test_knit_warning = function(text) {
    has_warning(knit(text = text, tangle = TRUE, quiet = TRUE))
  }
  code = c("#@a", "1+1", "#@b", "#@a", "rnorm(10)", "#@b")
  lines_basic_use = c( # should yield no error
    "```{r cache = FALSE}",
    "read_chunk(lines = code, labels = 'foo') # no issue to have comment after",
    "```"
  )
  lines_basic_use_with_knitr = c( # should yield no error
    "```{r cache = FALSE}",
    "knitr::read_chunk(lines = code, labels = paste0('foo')) # don't use read_chunk(\"nonexistent_file.R\")",
    "#code",
    "```",
    "```{r foo}",
    "```"
  )
  lines_file_commented_out = c( # should yield no error
    "```{r cache = FALSE}",
    "# knitr::read_chunk(\"nonexistent_file.R\")",
    "```"
  )
  lines_fake_function = c( # should yield no error
    "```{r cache = FALSE}",
    "fake_read_chunk <- function(lines){'(Hi!)'}",
    "# fake_read_chunk(lines = ')')",
    "```"
  )
#  lines_nonexist_file = c( # should yield error
#    "```{r nonexist-file, cache = FALSE}",
#    "knitr::read_chunk(\"nonexistent_file.R\")",
#    "```"
#  )
  (FALSE %==% test_knit_warning(text = lines_basic_use))
  (FALSE %==% test_knit_warning(text = lines_basic_use_with_knitr))
  (FALSE %==% test_knit_warning(text = lines_file_commented_out))
  (FALSE %==% test_knit_warning(text = lines_fake_function))
#  (has_error(knit(text = lines_nonexist_file, tangle = TRUE, quiet = TRUE), silent = TRUE) %==% TRUE)
})

code.matches = c(
  '# Should match',
  'read_chunk(path_variable)',
  'read_chunk("path/file.R")',
  'knitr::read_chunk("path/file.R")',
  'read_chunk("path/file.R"))))     # check extra parentheses',
  'read_chunk("path/file.R")        # this is helpful',
  'read_chunk(here::here("file.R")) # this is also helpful',
  'read_chunk("path/file.R")        # use read_chunk("oldpath/file.R")',
  'read_chunk("path/file.R")        # read_chunk("oldpath/file.R")',
  'read_chunk(here::here("file.R")) # use read_chunk',
  'read_chunk(here::here("file.R")) # use read_chunk("other.R")',
  'read_chunk(here::here(paste0("file.","R"))) # use read_chunk("other.R"',
  'read_chunk(',
  '  here::here(',
  '    paste0(#',
  '      paste0("file","."),#',
  '      "R"',
  '    )',
  '  )',
  ') # use read_chunk("other.R"))',
  'read_chunk(lines = "hello")      # dont use other_function()',
  'read_chunk(lines = "hello")      # dont use other_function()',
  'read_chunk(lines = "hello")      # dont use other_function()',
  'knitr::read_chunk("existent_file.R") # this is fine',
  '',
  '# Could match',
  'read_chunk(',
  '  here::here("R", # some code',
  '             "MS2.R")',
  ')'
)
code.nonmatches = c(
  '# Should not match (but bad practice)',
  'knutr::read_chunk("path/file.R") # user error though so can ignore',
  'read_chunk(lines = "hello"      # dont use other_function) # bad code',
  'read_chunk(lines = "hello".     ',
  '# dont use other_function) # bad code',
  '',
  '# Should not match',
  'read_chunk(lines = "(")',
  'read_chunk(lines = ")") # note matching across lines here due "(" and ")"',
  'read_chunk(lines = "(")',
  '# fake_read_chunk(lines = ")")',
  'rc <- read_chunk',
  'rc <- knitr::read_chunk',
  'rc <- read_chunk("file/path") # not very likely',
  'rc <- knitr::read_chunk("file/path") # not very likely',
  'fake_read_chunk(1)',
  '#read_chunk("anotherpath/fileold.R")',
  '# read_chunk("anotherpath/fileold.R")',
  '# read_chunk("path/file.R")      # use read_chunk',
  '# read_chunk(lines = "hello")',
  '# dont call read_chunk("file/path")',
  '# knitr::read_chunk("non_existent_file.R")',
  '#knitr::read_chunk("non_existent_file.R")',
  'fake_read_chunk <- function(lines){"Hi!"}',
  '# read_chunk(lines = ")")'
)
code = c(code.matches, code.nonmatches)
regex.orig = 'read_chunk\\(([^)]+)\\)'
regex.mine = '^[^#\\s]*\\bread_chunk\\([^#\\n]+\\)'
regex.matc = '[^#\\n]*\\bread_chunk\\((?:[^)(]|\\((?:[^)(]|\\((?:[^)(]|\\([^)(]*\\))*\\))*\\))*\\)'
(m = gregexpr(regex.matc, code, perl = TRUE))
unlist(regmatches(code, m))

(list.to.process = code.nonmatches)
m = gregexpr(regex.orig, list.to.process, perl = TRUE)
unlist(regmatches(list.to.process, m))

m = gregexpr(regex.mine, list.to.process, perl = TRUE)
unlist(regmatches(list.to.process, m))

m = gregexpr(regex.matc, list.to.process, perl = TRUE)
unlist(regmatches(list.to.process, m))

(extr = unlist(knitr:::str_extract(code, regex.orig)))
(extr = unlist(knitr:::str_extract(code, regex.mine)))
(extr = unlist(knitr:::str_extract(code, regex.matc)))

#read_chunk <- function(arg) {expression(arg)}

#xfun::parse_only(extr)

#eval(parse_only(unlist(str_extract(code, '^[^#\\s]*\\bread_chunk\\([^#\\n]+\\)'))))
