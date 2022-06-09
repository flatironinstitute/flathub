openapi.json:
	curl -o $@ http://localhost:8092/openapi.json

%: openapi.json
	docker run -v $(CURDIR):/data -u `id -u`:`id -g` --rm openapitools/openapi-generator-cli generate --package-name flathub -i /data/openapi.json -g $@ -o /data/$@
