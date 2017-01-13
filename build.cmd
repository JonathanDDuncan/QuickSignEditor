cd scripts
npm run build

del C:\Users\Jonathan\Documents\Freelance\SignWriterStudio\signmaker\lib\elm.js
copy .\dist\*.js  C:\Users\Jonathan\Documents\Freelance\SignWriterStudio\signmaker\lib\elm.js
copy .\src\styles\*.css  C:\Users\Jonathan\Documents\Freelance\SignWriterStudio\signmaker\css\
pause