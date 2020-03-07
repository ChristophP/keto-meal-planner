import {Elm} from './Main.elm'
import data from './data.json'


const app = Elm.Main.init({
  node: document.querySelector('#app'),
  flags: data
});

