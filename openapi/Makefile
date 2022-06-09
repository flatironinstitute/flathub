openapi.json:
	curl -o $@ http://localhost:8092/openapi.json

openapi-patched.json: openapi.json
	sed 's/"maximum":18446744073709551615,//g' $< > $@

Makefile:
	true

%: openapi-patched.json FORCE
	docker run -v $(CURDIR):/data -u `id -u`:`id -g` --rm openapitools/openapi-generator-cli generate --package-name flathub -p packageVersion=2.0 -i /data/$< -g $@ -o /data/$@

.PHONY: FORCE
