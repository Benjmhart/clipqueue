const iohook = require('iohook');
const fetch = require('node-fetch');

iohook.on('keydown', async e => {
  //console.log('thing happened', e);
  if (e.ctrlKey && !e.shiftKey && !e.metaKey && !e.altKey) {
    switch (e.keycode) {
      case 47:
        fetch(`http://localhost:44499/PasteEvent`).catch(err => console.log(err.message));
        fetch('http://localhost:55998', { method: 'post' }).catch(err => console.log(err.message));
        return console.log('PasteEvent');
      case 46:
        fetch(`http://localhost:44499/CopyEvent`).catch(err => console.log(err.message));
        fetch('http://localhost:55999', { method: 'post' }).catch(err => console.log(err.message));
        return console.log('copyEvent');
      case 45:
        fetch(`http://localhost:44499/CutEvent`).catch(err => console.log(err.message));
        fetch('http://localhost:55999', { method: 'post' }).catch(err => console.log(err.message));
        return console.log('cutEvent');
      default:
        return; //console.log('not handled', e.keycode);
    }
  }
});

iohook.start();
