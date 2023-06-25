elmMapbox.registerCustomElement({ token: "pk.eyJ1IjoiaHJrY3owMDEiLCJhIjoiY2xqYXRrZjFsMXZ5czNqcXkyMmhtenU4cSJ9.o2jwK7LYV_6QYMPmLJqaYQ"});
var app = Elm.Map.init({ node: document.getElementById("elm") });
elmMapbox.registerPorts(app);
