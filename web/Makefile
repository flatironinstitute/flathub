all: bundle.js.gz jspm_packages/npm/datatables.net-dt@1.10.21/css/jquery.dataTables.css.gz style.css.gz

TS:=$(wildcard *.ts)

# Build sass
bundle.js style.css: node_modules/.bin/tsc node_modules/.bin/sass $(TS) scss/main.scss $(wildcard scss/*/*.scss)
	-node_modules/.bin/tsc
	LC_ALL=en_US.UTF-8 npm run-script build

# Run npm install
node_modules/.bin/%:
	npm install

# Gzip everything
%.gz: %
	gzip -fk $< || gzip -c $< > $@
