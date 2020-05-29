import {Elm} from './Main.elm'
import storage from './storage'
import data from './data.json'

const ratioNoteSeenKey = 'ratioNoteSeen-29-06-2020'
const ratioNoteSeen = Boolean(localStorage.getItem(ratioNoteSeenKey)) || false;

const app = Elm.Main.init({
  node: document.querySelector('#app'),
  flags: {data, ratioNoteSeen}
});

app.ports.storeRatioNoteSeenStateLocally.subscribe(val =>
  localStorage.setItem(ratioNoteSeenKey, val)
);

// register SW
if ("serviceWorker" in navigator) {
  window.addEventListener("load", () => {
    navigator.serviceWorker
      .register("./sw.js")
      .then(registration => {
        console.log("SW registered: ", registration);
      })
      .catch(registrationError => {
        console.log("SW registration failed: ", registrationError);
      });
  });
}
