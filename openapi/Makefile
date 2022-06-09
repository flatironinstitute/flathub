openapi.json:
	curl -o $@ http://localhost:8092/openapi.json

Makefile:
	true

%: openapi.json FORCE
	docker run -v $(CURDIR):/data -u `id -u`:`id -g` --rm openapitools/openapi-generator-cli generate --package-name flathub -p packageVersion=2.0 -i /data/openapi.json -g $@ -o /data/$@

.PHONY: FORCE
