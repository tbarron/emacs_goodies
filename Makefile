DATE = $(shell date +"%Y.%m%d.%H%M%S")

help:
	@echo "   clean        remove emacs debris"
	@echo "   tarball      generate a tarball to transport my emacs goodies"

clean:
	find . -name "*~" | xargs rm -f

tarball:
	tar -c -z -v -f ~/tmp/el.$(DATE).tar.gz --exclude .git .

