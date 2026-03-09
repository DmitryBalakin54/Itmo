HaveCourse(StudentId, CourseId) :-
    Students(StudentId, _, GroupId),
    Plan(GroupId, CourseId, _).
HaveCourse(StudentId, CourseId) :-
    Marks(StudentId, CourseId, _).

HaveCourseName(StudentName, CourseName) :-
    Students(StudentId, StudentName, _),
    Courses(CourseId, CourseName),
    HaveCourse(StudentId, CourseId).