ForStudent(StudentId, CourseId) :-
    Marks(StudentId, CourseId, _).

ForGroupAndCourse(GroupId, CourseId) :-
    Groups(GroupId, _),
    Courses(CourseId, _),
    Students(StudentId, _, GroupId),
    not ForStudent(StudentId, CourseId).

AllHaveMark(GroupName, CourseName) :-
    Groups(GroupId, GroupName),
    Courses(CourseId, CourseName),
    not ForGroupAndCourse(GroupId, CourseId).