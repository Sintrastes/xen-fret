function download(text, name, type) {
    var a = document.createElement("a");
    var file = new Blob([text], {type: type});
    a.href = URL.createObjectURL(file);
    a.download = name;
    a.click();
    setTimeout(function() {
        document.body.removeChild(a);
    }, 0);
}

function storeAppData(contents) {
    if (typeof(Storage) !== "undefined") {
        window.localStorage.setItem("appData", contents);
    }
}

function getAppData() {
    if (typeof(Storage) !== "undefined") {
        return window.localStorage.getItem('appData');
    } else {
        return "";
    }
}

function setCookie(cname, cvalue, exdays) {
    const d = new Date();
    d.setTime(d.getTime() + (exdays*24*60*60*1000));
    let expires = "expires="+ d.toUTCString();
    document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

function getCookie(cname) {
    let name = cname + "=";
    let decodedCookie = decodeURIComponent(document.cookie);
    let ca = decodedCookie.split(';');
    for(let i = 0; i <ca.length; i++) {
      let c = ca[i];
      while (c.charAt(0) == ' ') {
        c = c.substring(1);
      }
      if (c.indexOf(name) == 0) {
        return c.substring(name.length, c.length);
      }
    }
    return "";
}

function importFile(callback) {
    var input = document.createElement('input');
    input.type = 'file';

    input.onchange = e => { 
        var file = e.target.files[0]; 

        var reader = new FileReader();
        reader.readAsText(file,'UTF-8');

        reader.onload = readerEvent => {
           var content = readerEvent.target.result;
           callback(content);
        }
    }

    input.click();
}

function bindColorPicker(colorPicker, onUpdate) {
    colorPicker.on('color:change', function(color) {
        onUpdate(color.hexString);
    });
}