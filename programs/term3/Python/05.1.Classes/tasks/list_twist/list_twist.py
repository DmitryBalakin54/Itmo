from collections import UserList
import typing as tp


class ListTwist(UserList[tp.Any]):
    """
    List-like class with additional attributes:
        * reversed, R - return reversed list
        * first, F - insert or retrieve first element;
                     Undefined for empty list
        * last, L -  insert or retrieve last element;
                     Undefined for empty list
        * size, S -  set or retrieve size of list;
                     If size less than list length - truncate to size;
                     If size greater than list length - pad with Nones
    """

    def __getattr__(self, item: str) -> tp.Any:
        match item:
            case 'size' | 'S':
                return len(self)
            case 'first' | 'F':
                return self[0]
            case 'last' | 'L':
                return self[-1]
            case "reversed" | 'R':
                return self[::-1]

    def __setattr__(self, item: str, val: tp.Any) -> None:
        super().__setattr__(item, val)
        match item:
            case 'size' | 'S':
                if val <= len(self):
                    self.data = self.data[0:val]
                else:
                    self.data.extend(None for _ in range(val - len(self)))
            case 'first' | 'F':
                self[0] = val
            case 'last' | 'L':
                self[-1] = val
