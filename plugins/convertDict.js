const fs = require("fs");

fs.readFile(process.env.HOME + "/.emacs.d/plugins/wb.txt", function (err, data) {
    if (err) console.log(err)
    var lines = data.toString().split("\n");
    var newLines = [
        "jfuj,2=%yyyy%-%MM%-%dd% %HH%:%mm%:%ss%\n",
        "jfuj,2=%HH%:%mm%:%ss%\n",
        "jfuj,2=%yyyy%年%M%月%d%日 %HH%点%mm%分%ss%秒\n",
        "jjad,2=%yyyy%-%MM%-%dd%\n",
        "jjad,2=%yyyy%年%M%月%d%日\n",
        "jjad,2=农历%M:lc%月%d:lc%\n\n",
        "jjad,2=%yyyy:c%年%M:c%月%d:c%日\n"
    ];
    for (var i = 44; i < lines.length; i++) {
        var codes = lines[i].split(" ");
        for (var j = 1; j < codes.length; j++) {
            newLines.push(codes[0] + ",1=" + codes[j] + "\n");
        }
    }

    fs.writeFile("C:/Program Files (x86)/Microsoft Bing Pinyin/1.6.98.04/Shared/userdefinephrase.dat", newLines.join(""), function (err) {
        if (err) console.log(err)
        else console.log("Write success!");
    })
})
