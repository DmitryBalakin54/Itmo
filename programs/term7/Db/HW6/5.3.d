HaveMark(StudentId, CourseId) :-
    Marks(StudentId, CourseId, _).

HaveLecturerCourse(StudentId) :-
    Students(StudentId, _, _),
    Lecturers(LecturerId, LecturerName),
    Plan(_, CourseId, LecturerId),
    LecturerName = :LecturerName,
    not HaveMark(StudentId, CourseId).

HaventLecturerCourse(StudentId) :-
    Students(StudentId, _, _),
    not HaveLecturerCourse(StudentId).