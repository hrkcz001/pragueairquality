'use strict';

import { Elm } from './src/Main.elm';
import {registerCustomElement} from "elm-mapbox";

registerCustomElement ({ token: "pk.eyJ1IjoiaHJrY3owMDEiLCJhIjoiY2xqYXRrZjFsMXZ5czNqcXkyMmhtenU4cSJ9.o2jwK7LYV_6QYMPmLJqaYQ"});
Elm.Main.init();
