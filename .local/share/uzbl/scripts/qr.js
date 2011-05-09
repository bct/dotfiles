(function() {
  var script = document.createElement('script');
  script.type = 'text/javascript';
  script.src = 'http://api.qrtag.net/js';
  document.documentElement.appendChild(script);

  script.onload = function() {
    var qrtag = new QRtag();
    qrtag.link(document.location);
    qrtag.image();
  };
})()
