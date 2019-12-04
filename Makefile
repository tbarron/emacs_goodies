DATE = $(shell date +"%Y.%m%d.%H%M%S")

help:
	@echo "   clean        remove emacs debris"
	@echo "   tarball      generate a tarball to transport my emacs goodies"
	@echo "   test-do-mode run tests for do-mode"
	@echo "   ert          run tests"

clean:
	find . -name "*~" | xargs rm -f

tarball:
	tar -c -z -v -f ~/tmp/el.$(DATE).tar.gz --exclude .git .

test-do-mode:
	emacs -batch -l ert -l test-do-mode.el --eval "(ert-run-tests-batch-and-exit \"$(SEL)\")"
	@echo "Use SEL=foo to select which tests to run"

