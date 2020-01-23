// Dark (inversion)
javascript:(function(d){d.head.appendChild(d.createElement('style')).innerText='html,img,video{-webkit-filter:invert(1)hue-rotate(180deg);filter:invert(1)hue-rotate(180deg)}body{background:#000}'})(document);

// Light (revert inversion)
javascript:(function(h){h.removeChild(h.lastChild)})(document.head);
