ForStudent(StudentId, CourseId) :-
    Marks(StudentId, CourseId, _).

ForGroupAndCourse(GroupId, CourseId) :-
    Groups(GroupId, _),
    Courses(CourseId, _),
    Students(StudentId, _, GroupId),
    not ForStudent(StudentId, CourseId).

AllHaveMark(GroupId, CourseId) :-
    Groups(GroupId, _),
    Courses(CourseId, _),
    not ForGroupAndCourse(GroupId, CourseId).