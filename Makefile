.PHONY: index.js deploy
index.js: src/**/*.elm
	elm make src/Whiteboard.elm --output index.js

deploy: index.js index.html
	mkdir -p dist
	cp index.{html,js} dist/
	aws --profile personal s3 sync --acl public-read ./dist s3://whiteboard.stephensugden.com/

