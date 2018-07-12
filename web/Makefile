all: index.js.gz jspm.config.js.gz jspm_packages/system.js.gz jspm_packages/npm/datatables.net-dt@1.10.19/css/jquery.dataTables.css.gz

index.js: main.ts
	npm run-script build

%.gz: %
	gzip -fk $< || gzip -c $< > $@
