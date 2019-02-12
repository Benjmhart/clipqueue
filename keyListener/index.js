const iohook = require('iohook');
const fetch = require('node-fetch');

iohook.on('keydown', e => {
  //console.log('thing happened', e);
  if (e.ctrlKey && !e.shiftKey && !e.metaKey && !e.altKey) {
    switch (e.keycode) {
      case 47:
        fetch(`http://localhost:44499/PasteEvent`).then(console.log);
        return console.log('PasteEvent');
      case 46:
        fetch(`http://localhost:44499/CopyEvent`).then(console.log);
        return console.log('copyEvent');
      case 45:
        fetch(`http://localhost:44499/CutEvent`).then(console.log);
        return console.log('cutEvent');
      default:
        return; //console.log('not handled', e.keycode);
    }
  }
});

iohook.start();
