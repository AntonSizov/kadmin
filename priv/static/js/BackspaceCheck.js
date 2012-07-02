function checkBackSpace(event) {
    event = event || window.event;
    var target = event.target || event.srcElement;

    if (event.keyCode == 8) {
        if (!(target.type == "text" || target.type == "textarea" || target.type == "password")) {
            if (confirm('You have pressed Backspace. Are you sure you want to go to the previous page?'))
                return true;
            return false;
        }
    }
}