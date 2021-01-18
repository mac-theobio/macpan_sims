## This is McMaster pandemic:  notes subdir

current: target
-include target.mk

# -include makestuff/perl.def

######################################################################

# Content

vim_session:
	bash -cl "vmt"

wrap_makeR=yes

######################################################################

Sources += $(wildcard *.R) Makefile

ignore += cachestuff/

cachestuff:
	git clone https://github.com/wzmli/MacPan_cache.git $@

testify_funs.Rout: testify_funs.R
	$(makeR)

batchtools.Rout: batchtools.R
	$(makeR)

simulate_fit.Rout: simulate_fit.R testify_funs.rda batchtools.rda
	$(makeR)

######################################################################

%.html: %.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format="html_document")'

%.pdf: %.Rnw
	Rscript -e 'knitr::knit2pdf("$<")'

######################################################################

### Makestuff

Sources += Makefile

Ignore += makestuff
msrepo = https://github.com/dushoff

makestuff:
	git clone https://github.com/dushoff/makestuff.git $@

-include makestuff/os.mk

-include makestuff/cacheflow.mk
-include makestuff/cacherepo.mk

-include makestuff/makeR.mk
-include makestuff/rmd.mk
-include makestuff/yushan.mk
-include makestuff/nodes.mk

-include makestuff/git.mk
-include makestuff/visual.mk
-include makestuff/projdir.mk

