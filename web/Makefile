all: index.js.gz jspm.config.js.gz jspm_packages/system.js.gz jspm_packages/npm/datatables.net-dt@1.10.19/css/jquery.dataTables.css.gz

index.js: node_modules/.bin/tsc main.ts
	-node_modules/.bin/tsc
	npm run-script build

node_modules/.bin/%:
	npm install

%.gz: %
	gzip -fk $< || gzip -c $< > $@
