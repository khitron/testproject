/** kate-script
 * name: PathLP
 * author: Igal Khitron
 * revision: 1
 * kate-version: 3.0
 * type: indentation
 * indent-languages: PathLP
 *
 * PathLP Kate indentation javascript
 */

triggerCharacters = "/*";

var answer = -1,
        incomment = false;

function exit(state) {
        if (state == "opened") {
                return answer;
        }
        return 0;
}

function delimiter(char) {
        return (char == ' ' || char == '\t' || char == '\r');
}

function evaluate(all, state, flag) {
        if (state == "closed") {
                return closed(all, flag);
        }
        if (state == "opened") {
                return opened(all, flag);
        }
        return suspect(all);
}

function comment(state, all) {
        incomment = true;
        if (all.length == 0) {
                return exit(state);
        }
        if (all.length > 1 && all.charAt(0) == '*' && all.charAt(1) == '/') {
                incomment = false;
                return evaluate(all.substring(2), state, false);
        }
        return comment(state, all.substring(1));
}

function linecomment(state, all) {
        if (all.charAt(0) == '\n') {
                return evaluate(all.substring(1), state, true);
        }
        return linecomment(state, all.substring(1));
}

function backquote(all) {
        if (all.length == 0) {
                return exit("closed");
        }
        if (all.charAt(0) == '`') {
                return closed(all.substring(1), false);
        }
        return backquote(all.substring(1));
}

function opened(all, flag) {
        if (all.length == 0) {
                return exit("opened");
        }
        var first = all.charAt(0), rest = all.substring(1);
        if ((delimiter(first) && flag == true) || first == '\n') {
                return opened(rest, true);
        }
        if ((first == '#' && flag == true) || first == '%') {
                return linecomment("opened", rest);
        }
        if (first == ';') {
                return suspect(rest);
        }
        if (first == '/' && rest.length > 0 && rest.charAt(0) == '*') {
                return comment("opened", rest.substring(1));
        }
        return opened(rest, false);
}

function closed(all, flag) {
        if (all.length == 0) {
                return exit("closed");
        }
        var first = all.charAt(0), rest = all.substring(1);
        if ((first == '#' && flag == true) || first == '%') {
                return linecomment("closed", rest);
        }
        if (delimiter(first) || first == ';') {
                return closed(rest, false);
        }
        if (first == '\n') {
                return closed(rest, true);
        }
        if (first == '/' && rest.length > 0 && rest.charAt(0) == '*') {
                return comment("closed", rest.substring(1));
        }
        if (first == '`') {
                return backquote(rest);
        }
        return opened(rest, false);
}

function suspect(all) {
        var first = all.charAt(0), rest = all.substring(1);
        if (delimiter(first) || first == ';') {
                return suspect(rest);
        }
        if (first == '\n') {
                return closed(rest, true);
        }
        if (first == '/' && rest.length > 0 && rest.charAt(0) == '*') {
                return comment("suspect", rest.substring(1));
        }
        if (first == '%') {
                return linecomment("closed", rest);
        }
        return opened(rest, false);
}

function trysign(all, counter) {
        if (all.length < 2) {
                return -1;
        }
        if ((all.charAt(0) == ':' || all.charAt(0) == '?'
                        || all.charAt(0) == '!') && all.charAt(1) == '-') {
                return counter + 3;
        }
        return trysign(all.substring(1), counter + 1);
}

function findcomm(all, position, counter) {
        if (all.length < 2) {
                return position;
        }
        if (all.charAt(0) == '/' && all.charAt(1) == '*') {
                return counter + 1;
        }
        return findcomm(all.substring(1), position, counter + 1);
}

function indent(line, indentWidth, char) {
        if (char == '/') {
                var first = document.firstColumn(line),
                        last = document.lastColumn(line);
                if (document.line(line).substring(first, last + 1) == "* /"
                                && incomment == true) {
                        document.removeText(line, last - 1, line, last);
                }
                return -2;
        }
        if (char == '*') {
                var first = document.firstColumn(line),
                        last = document.lastColumn(line);
                if (document.line(line).substring(last - 1, last + 1) == "/*") {
                        document.insertText(line, last + 1, ' ');
                }
                return -2;
        }
        var prev = document.prevNonEmptyLine(line),
                fullString = "",
                lineCounter = 0,
                query = -1;
        if (prev == line) {
                prev = document.prevNonEmptyLine(line - 1);
        }
        var prevLine = document.line(prev);
        if (prev > -1) {
                query = document.firstColumn(prev);
                answer = trysign(prevLine, 0);
                if (query == 0 && answer == -1) {
                        answer = 8;
                } else {
                        answer = Math.max(answer, query);
                }
        }
        if (answer < 0) {
                answer = 3;
        }
        while (lineCounter < line) {
                fullString = fullString + document.line(lineCounter) + "\n";
                lineCounter = lineCounter + 1;
        }
        var fixed = closed(fullString, true);
        if (incomment == true) {
                document.insertText(line, view.cursorPosition().column, ' * ');
                return findcomm(prevLine, query, 0);
        }
        return fixed;
}
