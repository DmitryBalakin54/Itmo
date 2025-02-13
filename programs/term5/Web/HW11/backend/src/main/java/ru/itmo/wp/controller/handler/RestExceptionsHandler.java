package ru.itmo.wp.controller.handler;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import ru.itmo.wp.exception.ValidationException;

@RestControllerAdvice
public class RestExceptionsHandler {

    @ExceptionHandler(ValidationException.class)
    public ResponseEntity<String> onValidationException(ValidationException validationException){
        return new ResponseEntity<>(toError(validationException), HttpStatus.BAD_REQUEST);
    }

    private String toError(ValidationException validationException) {
        if (validationException.getBindingResult().hasErrors()){
            return validationException.getBindingResult().getAllErrors().get(0).getDefaultMessage();
        }
        throw new RuntimeException("Unexpected Error");
    }

}
