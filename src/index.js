import {Elm} from './Main.elm'
import data from './clean-data-2.json'


const app = Elm.Main.init({
  node: document.querySelector('#app'),
  flags: data
});

