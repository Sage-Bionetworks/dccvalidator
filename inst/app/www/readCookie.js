// read the user login token from a cookie
Shiny.addCustomMessageHandler("readCookie", function(message) {
  readCookie();
});

function readCookie() {
  const xhr = new XMLHttpRequest();
  const url='https://www.synapse.org/Portal/sessioncookie';
  xhr.withCredentials = true;
  xhr.onreadystatechange = function() {
    if (xhr.readyState == XMLHttpRequest.DONE) {
      if (xhr.status == 401) {
        Shiny.onInputChange("authorized", false);
      } else {
        Shiny.onInputChange("cookie",xhr.responseText);
      }
    };
  };
  xhr.open("GET", url);
  xhr.send();
}
