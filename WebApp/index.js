'use strict';

import { Elm } from './src/Main.elm';
import {registerCustomElement} from "elm-mapbox";

const LOCAL_STORAGE_TOKEN_KEY = 'token';

registerCustomElement({ token: "pk.eyJ1IjoiaHJrY3owMDEiLCJhIjoiY2xqYXRrZjFsMXZ5czNqcXkyMmhtenU4cSJ9.o2jwK7LYV_6QYMPmLJqaYQ"});
const app = Elm.Main.init({
  flags: localStorage.getItem(LOCAL_STORAGE_TOKEN_KEY),
});
