# THIS IS A STUB FILE FOR THE SCANNER
# STUDENTS WILL BE FILLING IN THE DETAILS

from tau.tokens import Token, Span, Coord, keywords, punctuation
from tau.error import ScanError
from typing import Iterator
import string


class Scanner:
    tokens: list[Token]

    def checkNewChar(self, type: int, lastChar: str, newChar: str) -> bool:
        if type == 1:
            if newChar not in string.ascii_letters and newChar not in string.digits:
                return True
            else:
                return False
        elif type == 2:
            if newChar not in string.digits:
                return True
            else:
                return False
        elif type == 3:
            if lastChar + newChar not in punctuation:
                return True
            else:
                return False
        return False  # This should never happen but mypy kept complaining

    def makeToken(self, type: int, value: str, span: Span) -> None:
        kind = ""
        if type == 1:
            kind = "ID"
        elif type == 2:
            kind = "INT"
        if value in keywords or value in punctuation:
            self.tokens.append(
                Token(
                    value,
                    value,
                    span,
                )
            )
        else:
            self.tokens.append(
                Token(
                    kind,
                    value,
                    span,
                )
            )

    def __init__(self, input: str):
        self.tokens = []
        curLine = 1
        curCol = 1
        startCol = 1
        startIndex = 0
        # For curType, 1 indicates that the current token is an identifier and 2 means it's an integer literal.
        # 0 indicates that a token was just finished so the next character will define the new token's type.
        curType = 0
        i = 0
        length = len(input)
        endIsComment = False
        while i < length:
            if input[i] == "/" and i + 1 < length and input[i + 1] == "/":
                while i < length and input[i] != "\n":
                    i += 1
                    curCol += 1
                if i == length:
                    endIsComment = True
            elif input[i] == "!" and i + 1 < length and input[i + 1] == "=":
                if curType != 0:
                    self.makeToken(
                        curType,
                        input[startIndex:i],
                        Span(Coord(curLine, startCol), Coord(curLine, curCol)),
                    )
                self.makeToken(
                    3,
                    input[i : i + 2],
                    Span(Coord(curLine, curCol), Coord(curLine, curCol + 2)),
                )
                curCol += 2
                startIndex = i + 1
                i += 2
                curType = 0
            elif curType == 0:
                if input[i] not in string.whitespace:
                    startIndex = i
                    startCol = curCol
                    if input[i] in string.ascii_letters:
                        curType = 1
                    elif input[i] in string.digits:
                        curType = 2
                    elif input[i] in punctuation:
                        curType = 3
                    else:
                        raise ScanError(
                            "Incorrect Syntax",
                            Span(Coord(curLine, startCol), Coord(curLine, curCol)),
                        )
                    curCol += 1
                else:
                    if input[i] == "\n":
                        curCol = 1
                        curLine += 1
                    else:
                        curCol += 1
                i += 1
            elif input[i] in string.whitespace:
                self.makeToken(
                    curType,
                    input[startIndex:i],
                    Span(Coord(curLine, startCol), Coord(curLine, curCol)),
                )
                curCol += 1
                curType = 0
                if input[i] == "\n":
                    curCol = 1
                    curLine += 1
                i += 1
            elif self.checkNewChar(curType, input[i - 1], input[i]):
                self.makeToken(
                    curType,
                    input[startIndex:i],
                    Span(Coord(curLine, startCol), Coord(curLine, curCol)),
                )
                if input[i] in string.ascii_letters:
                    curType = 1
                elif input[i] in string.digits:
                    curType = 2
                elif input[i] in punctuation:
                    curType = 3
                startCol = curCol
                curCol += 1
                startIndex = i
                i += 1
            else:
                curCol += 1
                i += 1
        if length > 0 and input[-1] not in string.whitespace and endIsComment == False:
            if length > 1 and input[-2] not in string.whitespace:
                if self.checkNewChar(curType, input[-2], input[-1]):
                    self.makeToken(
                        curType,
                        input[startIndex:-1],
                        Span(Coord(curLine, startCol), Coord(curLine, curCol)),
                    )
                    if input[-1] in string.ascii_letters:
                        curType = 1
                    elif input[-1] in string.digits:
                        curType = 2
                    elif input[-1] in punctuation:
                        curType = 3
                    self.makeToken(
                        curType,
                        input[-1],
                        Span(Coord(curLine, curCol + 1), Coord(curLine, curCol + 1)),
                    )
                else:
                    self.makeToken(
                        curType,
                        input[startIndex:],
                        Span(Coord(curLine, startCol), Coord(curLine, curCol)),
                    )
            else:
                if length > 1 and input[-2] == "\n":
                    curCol = 0
                self.makeToken(
                    curType,
                    input[-1:],
                    Span(Coord(curLine, curCol + 1), Coord(curLine, curCol + 2)),
                )
        if curCol == 0:
            curCol = 2
        self.tokens.append(
            Token(
                "EOF",
                "",
                Span(Coord(curLine, curCol), Coord(curLine, curCol)),
            )
        )
        pass

    def __iter__(self) -> Iterator[Token]:
        return iter(self.tokens)
