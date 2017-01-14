"copy over.cmd"  
rem Please commit any changes to the publish folder
pause
git branch -dr origin/gh-pages
git subtree push --prefix published origin gh-pages

pause