function DateValidatorAllowEmpty(sender, args) {
    var pickerid = "";

    for (var i = 0; i < sender.attributes.length; i++) {
    if (sender.attributes[i].name == "controltovalidate")
        pickerid = sender.attributes[i].value;
    }

    if (pickerid == "")
    return;

    var picker = document.getElementById(pickerid);
    if (picker == null)
        return;
    var txtPicker = document.getElementById(pickerid + "_text");

    if (txtPicker == null) {
        txtPicker = picker;          
    }

    var validator = document.getElementById(sender.id);

    if (validator.innerText.indexOf(" Date must be between 01/01/1753 and 12/31/9999.") < 0)
    validator.innerText = validator.innerText + " Date must be between 01/01/1753 and 12/31/9999.";
      
    if( picker.value == txtPicker.value )
    {
    if( picker.value == "" ) return;
    }
    else
    {
        if (txtPicker.value == "") {
            picker.value = txtPicker.value;
            return;
        }
    }

    DateValidator(sender, args);
}

function DateValidator(sender, args) {
    var pickerid = "";

    for (var i = 0; i < sender.attributes.length; i++) {
    if (sender.attributes[i].name == "controltovalidate")
        pickerid = sender.attributes[i].value;
    }

    if (pickerid == "")
    return;

    var picker = document.getElementById(pickerid);
    if (picker == null)
        return;
    var txtPicker = document.getElementById(pickerid + "_text");

    if (txtPicker == null) {
        txtPicker = picker;
    }

    var validator = document.getElementById(sender.id);

    if (validator.innerText.indexOf(" Date must be between 01/01/1753 and 12/31/9999.") < 0)
    validator.innerText = validator.innerText + " Date must be between 01/01/1753 and 12/31/9999.";

    if (picker != txtPicker && picker.value == txtPicker.value) {
    var date = picker.value.split("/");

    if (date[2].length == 2) {
        picker.value = date[0] + "/" + date[1] + "/" + "19" + date[2];
        date[2] = "19" + date[2];
        txtPicker.value = picker.value;
    }
    if (date[2].length != 4) {
        args.IsValid = false;
        txtPicker.focus();
        return;
    }
    var year = parseInt(date[2]);
    if (year < 1753) {
        args.IsValid = false;
        txtPicker.focus();
        return;
    }

    return;
    }
    else {
    txtPicker.value = Trim(txtPicker.value, " ");
    var valid = CheckDate(txtPicker.value);

    if (!valid) {
        args.IsValid = false;
        txtPicker.focus();
        return;
    }

    picker.value = txtPicker.value;
    }
}

function CheckDate(dtStr) {
    var date = dtStr.split("/");

    if (!IsPickedDateValid(date)) {
    return false;
    }

    FormatDateParts(date);

    var month = parseInt(date[0]);
    var day = parseInt(date[1]);
    var year = parseInt(date[2]);
    var minYear = 1753;
    var maxYear = 9999;
    var daysArray = DaysArray(year);

    if (!(month > 0 && day > 0 && year > 0) || year < minYear || year > maxYear || month > 12 || day > daysArray[month])
    return false;

    return true;
}

function DaysArray(year) {
    for (var i = 1; i <= 13; i++) {
    this[i] = 31;
    if (i == 4 || i == 6 || i == 9 || i == 11)
        this[i] = 30;
    if (i == 2) {
        if (year % 4 == 0)
        this[i] = 29;
        else
        this[i] = 28;
    }
    }

    return this;
}

function IsInteger(s) {
    var patt = /\D/g;
    var notDig = s.match(patt);
    if (notDig == null && s != "")
    return true;
    return false;
}


function IsPickedDateValid(pickedDate) {
    if (pickedDate.length != 3 || pickedDate[0].length > 2 || pickedDate[1].length > 2 || pickedDate[2].length > 4) {
    return false;
    }

    if (!IsInteger(pickedDate[0]) || !IsInteger(pickedDate[1]) || !IsInteger(pickedDate[2])) {
    return false;
    }

    return true;
}

function FormatDateParts(dateParts) {
    if (dateParts[0].charAt(0) == "0" && dateParts[0].length > 1) {
    dateParts[0] = dateParts[0].substring(1);
    }

    if (dateParts[1].charAt(0) == "0" && dateParts[1].length > 1) {
    dateParts[1] = dateParts[1].substring(1);
    }

    for (var i = 1; i <= 3; i++) {
    if (dateParts[2].charAt(0) == "0" && dateParts[2].length > 1) {
        dateParts[2] = dateParts[2].substring(1);
    }
    }
}

function FireDefaultButton(event, buttonID) {
    if (event.keyCode == 13) {
    var searchButton = document.getElementById(buttonID);
    if (searchButton != null) {
        searchButton.click();
        event.cancelBubble = true;
        return false;
    }
    }

    return true;
}
