{ a[NR,1] = $1; 
  a[NR,2] = $2; 
  a[NR,3] = $3 
} 

END {
  print NR;
  for (i=1;i<=3;i++)  {
    for (j=1;j<=NR;j++) 
      printf ("%d ", a[j,i]);
    printf ("\n");
  }
}
