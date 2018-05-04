import 'dart:async';

main() async {
  print('Hello');
  String str1 = ''' hello ''';
  var str2 = """
       "dlkj " dslkj

dlskj'dsd''d f'sd' 'd'fsthis is omething to
""";

  var str3 = r"this is a raw string\n\n";

  for (int i = 0; i < 2; i++) {
    print('hello $str1!\n${i + 1}');
    print("hello ${str2}");
    print("yo $str3");
  }
}
