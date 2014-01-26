
function add_sensor_name(sensor_id, name){
    var $parent = $("#" + sensor_id+' .sensor-name-cell');


    $('<span/>')
        .addClass("sensor-name")
        .text(name)
        .appendTo($parent) ;

    var $btn =
    $('<button/>')
        .addClass("edit-btn")
        .text("Edit")
        .click(function() { edit_sensor_name(sensor_id); });
    $btn.appendTo($parent);

}

function edit_sensor_name(sensor_id){
    var $editBtn = $('#'+sensor_id+" .edit-btn");
    var $text = $('#'+sensor_id+" .sensor-name");
    var $input = $('<input>');
    $input.val($text.text());

    var $parent = $("#" + sensor_id+' .sensor-name-cell');

        function submit_sensor_name() {
            $text.text($input.val());
            $parent.empty();
            $text.appendTo($parent);
            $editBtn.click(function() { edit_sensor_name(sensor_id)})
            $editBtn.appendTo($parent);
       }

    $input.focusout(submit_sensor_name);
    $parent.empty();
    $input.appendTo($parent);

}

function add_sensor(uuid){

    var newRow = $('<tr><td id="sensor_name_"' + uuid + '>       \
    </td> \
        <td><button>X</button></td> \
        <td>2014 <button id="sensor' + uuid + '_deleteall">Del</button></td> \
        <td>2 min</td> \
        <td>Ok <button id="sensor' + uuid + '_confupl">Configure</button></td></tr> \
    ');
    var row = $("<tr></tr>", {id: uuid});
    row.append($("<td class='sensor-name-cell'></td>"));
    row.append($("<td><button>X</button></td>"));
    row.append($("<td>20134 <button class='delete-btn'>Del</button></td>"));
    row.append($("<td>2 min</td>"));
    row.append($("<td>Ok <button class='configure-btn'>Configure</button></td>"));

    $("#sensors").find("tbody").append(row);
    add_sensor_name(uuid, "sensor 1");
}
function add_new_sensor(){
    var numRows = $("#sensors").find("tr").length;
    add_sensor(numRows + 1);
}

