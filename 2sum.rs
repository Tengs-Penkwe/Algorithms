pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
    let num1 = nums1.len();
    let num2 = nums2.len();
    let total = num1 + num2;

    let mut idx1 = 0;
    let mut idx2 = 0;

    let mut first = 0;
    let mut second = 0;//if nums1[0] < nums2[0] {nums2[0]} else {nums1[0]};

    while idx1 + idx2 < (total + 1) / 2 {
        first = second;

        if idx1 == num1 - 1 {
            second = nums2[idx2];
            idx2 += 1;
        } else if idx2 == num2 - 1 {
            second = nums1[idx1];
            idx1 += 1;
        } else {
            if nums1[idx1] < nums2[idx2] {
                second = nums1[idx1];
                idx1 += 1;
            } else {
                second = nums2[idx2];
                idx2 += 1;
            }
        }
    }

    if total % 2 == 0 {
        (first + second) as f64 / 2.0
    } else {
        second as f64
    }
}
    

// Write test cases here
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1() {
        assert_eq!(find_median_sorted_arrays(vec![1, 3], vec![2]), 2.0);
    }

    #[test]
    fn test_2() {
        assert_eq!(find_median_sorted_arrays(vec![1, 2], vec![3, 4]), 2.5);
    }

    #[test]
    fn test_3() {
        assert_eq!(find_median_sorted_arrays(vec![0, 0], vec![0, 0]), 0.0);
    }

    #[test]
    fn test_4() {
        assert_eq!(find_median_sorted_arrays(vec![], vec![1]), 1.0);
    }

    #[test]
    fn test_5() {
        assert_eq!(find_median_sorted_arrays(vec![2], vec![]), 2.0);
    }
}
