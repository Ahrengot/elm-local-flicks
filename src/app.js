import "./css/main.css"

import Elm from "./Main.elm"

var elm = Elm.Main.fullscreen({
  title: "Local Flickr's",
  flickrApiKey: "707d2f80846451df31cf7f4b9b19a525",
  gmapsApiKey: "707d2f80846451df31cf7f4b9b19a525"
});

elm.ports.changeBodyBg.subscribe( newColor => {
  document.body.style.backgroundColor = newColor;
} )
