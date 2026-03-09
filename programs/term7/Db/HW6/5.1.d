HaveLecturerCourse(StudentId) :-
    Students(StudentId, _, GroupId),
    Lecturers(LecturerId, LecturerName),
    Marks(StudentId, CourseId, _),
    Plan(GroupId, CourseId, LecturerId),
    LecturerName = :LecturerName.