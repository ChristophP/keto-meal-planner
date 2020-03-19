import {Elm} from './Main.elm'
import data from './data.json'


const app = Elm.Main.init({
  node: document.querySelector('#app'),
  flags: data
});

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
