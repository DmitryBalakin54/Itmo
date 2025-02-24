package info.kgeorgiy.ja.balakin.student;


import info.kgeorgiy.java.advanced.student.StudentQuery;
import info.kgeorgiy.java.advanced.student.Student;
import info.kgeorgiy.java.advanced.student.GroupName;

import java.util.*;
import java.util.stream.*;
import java.util.function.*;

public class StudentDB implements StudentQuery {

    private static final Comparator<Student> NAMES_COMPARATOR = Comparator.comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .thenComparing(Student::getGroup)
            .thenComparing(Comparator.naturalOrder());

    private static  <T, A, R> R collect(Collection<? extends T> collection,
                                        Function<? super T, ? extends A> mapper,
                                        Collector<? super A, ?, ? extends R> collector) {
        return collection.stream().map(mapper).collect(collector);
    }

    private static <T> List<T> collectToList(Collection<? extends Student> collection,
                                        Function<? super Student, T> mapper) {
        return collect(collection, mapper, Collectors.toList());
    }

    private static <T, R> R sort(Collection<? extends T> collection,
                                 Comparator<? super T> comparator,
                                 Collector<? super T, ?, ? extends R> collector) {
        return collection.stream().sorted(comparator).collect(collector);
    }

    private static List<Student> sortToList(Collection<? extends Student> collection,
                                 Comparator<? super Student> comparator) {
        return sort(collection, comparator, Collectors.toList());
    }

    private static List<Student> findWithNamesOrderToList(Collection<Student> collection,
                                                          Predicate<Student> predicate) {
        return collection.stream().filter(predicate).sorted(NAMES_COMPARATOR).collect(Collectors.toList());
    }


    @Override
    public List<String> getFirstNames(List<Student> students) {
        return collectToList(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return collectToList(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return collectToList(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return collectToList(students, s -> s.getFirstName() + " " + s.getLastName());
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return collect(students, Student::getFirstName, Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Comparator.naturalOrder()).map(Student::getFirstName).orElse("");
    }
    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortToList(students, Comparator.comparing(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortToList(students, NAMES_COMPARATOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return findWithNamesOrderToList(students, s -> s.getFirstName().equals(name));
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return findWithNamesOrderToList(students, s -> s.getLastName().equals(name));
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return findWithNamesOrderToList(students, s -> s.getGroup().equals(group));
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return findStudentsByGroup(students, group).stream().collect(
                Collectors.toMap(Student::getLastName,
                        Student::getFirstName,
                        BinaryOperator.minBy(String::compareTo)));
    }
}
