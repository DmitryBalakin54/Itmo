def find_value(nums: list[int] | range, value: int) -> bool:
    """
    Find value in sorted sequence
    :param nums: sequence of integers. Could be empty
    :param value: integer to find
    :return: True if value exists, False otherwise
    """

    left = -1
    right = len(nums)

    while left + 1 < right:
        mid = (left + right) // 2
        if value < nums[mid]:
            right = mid
        else:
            left = mid

    if left == -1:
        return False
    elif nums[left] == value:
        return True
    else:
        return False
