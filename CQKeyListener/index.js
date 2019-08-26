const iohook = require('iohook');
const fetch = require('node-fetch');

iohook.on('keydown', async e => {
  //console.log('thing happened', e);
  if (e.ctrlKey && !e.shiftKey && !e.metaKey && !e.altKey) {
    switch (e.keycode) {
      case 47:
        return sendEvent('PasteEvent');
      case 46: // copy, but cut behaviour is the same
        return sendEvent('CutEvent');
      case 45:
        return sendEvent('CutEvent');
      default:
        return; //console.log('not handled', e.keycode);
    }
  }
});

const evPorts = {
  PasteEvent: '55998',
  CutEvent: '55999'
};

const sendEvent = evStr => {
  fetch('http://localhost:' + evPorts[evStr], { method: 'post' }).catch(err => console.log(err.message));
  return console.log(evStr);
};

iohook.start();
