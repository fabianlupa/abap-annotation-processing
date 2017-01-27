#!/bin/bash -e

git config --global user.name "Travis CI"
git config --global user.email "builds@travis-ci.com"

openssl aes-256-cbc -K $encrypted_2a1c10c99a60_key -iv $encrypted_2a1c10c99a60_iv \
        -in ghpages.key.enc -out ghpages.key -d
chmod 600 ghpages.key
printf "%s\n" \
       "Host github.com" \
       "  IdentityFile ghpages.key" \
       "  LogLevel ERROR" >> ~/.ssh/config

cp -r ./abapdoc /tmp/abapdoc

git clean -fdx

git checkout origin/gh-pages
rm -rf ./_abapdoc/*
cp -r /tmp/abapdoc/* ./_abapdoc/

# Remove generation date footer so it does not show up as a diff
for file in $(find ./_abapdoc -name '*.html');
do
  sed -E 's/<div id="footer">Generated on [[:digit:]]{2}.[[:digit:]]{2}.[[:digit:]]{4}<\/div>//g' \
      $file > "${file}.tmp" && mv "${file}.tmp" $file
done


# Update only if there is a difference
if ! git diff-index --quiet HEAD ; then
  echo "Updating ABAP Doc on GitHub pages"

  git add -A
  git commit -m "Update ABAP Doc ($TRAVIS_BUILD_NUMBER)"
  git push -q git@github.com:flaiker/abap-annotation-processing gh-pages
fi
