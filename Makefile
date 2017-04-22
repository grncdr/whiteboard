.PHONY: index.js
index.js: src/**/*.elm
	elm make src/Whiteboard.elm --output index.js

deploy:
	cp index.{html,js} ~/Dropbox/stephensugden.com/whiteboard/
	cd ~/Dropbox/stephensugden.com && make deploy

