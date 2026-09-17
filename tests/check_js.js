const vm = require('vm');
const fs = require('fs');

const files = [
  'frontend/js/app.js',
  'frontend/js/export.js',
  'frontend/js/lists.js',
  'frontend/js/quiz.js',
  'frontend/js/compare.js'
];

let allOk = true;
files.forEach(f => {
  try {
    const code = fs.readFileSync(f, 'utf8');
    new vm.Script(code);
    console.log('[SYNTAX OK]:', f);
  } catch (err) {
    console.error('[SYNTAX ERROR in ' + f + ']:', err);
    allOk = false;
  }
});

if (!allOk) {
  process.exit(1);
} else {
  console.log('ALL JAVASCRIPT FILES VALIDATED WITH 0 SYNTAX ERRORS!');
}
