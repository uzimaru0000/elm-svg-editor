'use struct'

import './index.html'
import { Elm } from './Elm/Main.elm'

const main = document.getElementById('main');
const app = Elm.Main.init({
    node: main
});
