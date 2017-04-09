import "./css/main.css"

import Elm from "./Main.elm"

const debounce = (func, wait, immediate) => {
	var timeout;
	return function() {
		var context = this, args = arguments;
		var later = function() {
			timeout = null;
			if (!immediate) func.apply(context, args);
		};
		var callNow = immediate && !timeout;
		clearTimeout(timeout);
		timeout = setTimeout(later, wait);
		if (callNow) func.apply(context, args);
	};
};

const elm = Elm.Main.fullscreen({
  title: "Local Flickr's",
  flickrApiKey: "707d2f80846451df31cf7f4b9b19a525"
});

elm.ports.changeBodyBg.subscribe(newColor => {
  document.body.style.backgroundColor = newColor;
})

const debouncedSendScroll = debounce(() => {
	const bottomOfWindow = window.pageYOffset + window.innerHeight;
	const didReachEnd = bottomOfWindow >= document.body.clientHeight;
	elm.ports.onScroll.send([bottomOfWindow, didReachEnd]);
}, 350, false);

elm.ports.toggleScroll.subscribe(isActive => {
	if ( isActive ) {
		window.addEventListener('scroll', debouncedSendScroll);
	} else {
		window.removeEventListener('scroll', debouncedSendScroll);
	}
});

// Send window position as soon as app boots
debouncedSendScroll();
