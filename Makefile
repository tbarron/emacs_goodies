DATE = $(shell date +"%Y.%m%d.%H%M%S")

tarball:
	tar -c -z -v -f ~/tmp/el.$(DATE).tar.gz --exclude .git .

